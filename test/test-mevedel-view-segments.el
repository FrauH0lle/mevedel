;;; test-mevedel-view-segments.el -- Historical segment view tests -*- lexical-binding: t -*-

;;; Commentary:

;; Historical session segment projection and navigation coverage.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-session-artifacts)
(require 'mevedel-structs)
(require 'mevedel-tool-render-data)
(require 'mevedel-transcript-audit)
(require 'mevedel-view)
(require 'mevedel-view-render)
(require 'mevedel-view-segments)

(defun mevedel-view-segments-test--write
    (path prompt response fork-point-id segment &optional response-bound-length)
  "Write one persisted transcript segment to PATH.
PROMPT and RESPONSE form one settled turn identified by FORK-POINT-ID in
SEGMENT.  RESPONSE-BOUND-LENGTH may simulate a stale persisted response end."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
    (insert prompt "\n")
    (let ((response-start (point)) render-start render-end audit-start audit-end)
      (insert response "\n")
      (insert (mevedel-tool-render-data-format
               '(:kind request-summary)))
      (insert
       (mevedel--format-hook-audit-record
        (list :type 'fork-point
              :fork-point-id fork-point-id
              :segment segment
              :turn 1
              :file-turn 1
              :cum-turn segment)))
      (dotimes (_ 8)
        (goto-char (point-min))
        (search-forward response)
        (setq response-start (match-beginning 0))
        (search-forward "<!-- mevedel-render-data -->")
        (setq render-start (match-beginning 0))
        (search-forward "<!-- /mevedel-render-data -->")
        (setq render-end (point))
        (search-forward "<!-- mevedel-hook-audit -->")
        (setq audit-start (match-beginning 0))
        (search-forward "<!-- /mevedel-hook-audit -->")
        (setq audit-end (point))
        (let ((bounds
               (append
                (list
                 (list 'response
                       (list response-start
                             (+ response-start
                                (or response-bound-length
                                    (length response)))))
                 (list 'mevedel-render-data (list render-start render-end))
                 (list 'mevedel-hook-audit (list audit-start audit-end)))
                (when response-bound-length
                  (list
                   (list 'ignore
                         (list (+ response-start response-bound-length 2)
                               (+ response-start (length response)))))))))
          (org-entry-put (point-min) "GPTEL_BOUNDS"
                         (prin1-to-string bounds)))))
    (write-region (point-min) (point-max) path nil 'silent)))

(defmacro mevedel-view-segments-test--with-view (&rest body)
  "Run BODY in a rendered three-segment view with two archived segments."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "mevedel-view-segments-" t))
          (session
           (mevedel-session--create
            :authority-mode 'pid-lock
            :name "segments"
            :save-path (file-name-as-directory directory)
            :current-segment 3
            :prompt-index
            '((1 . ((:cum-turn 1 :preview "first prompt")))
              (2 . ((:cum-turn 2 :preview "second prompt")))
              (3 . ((:cum-turn 3 :preview "live prompt")))))))
     (unwind-protect
         (progn
           (mevedel-view-segments-test--write
            (mevedel-session-artifacts-segment-path directory 1)
            "First prompt" "Archived answer one" "fork-1" 1)
           (mevedel-view-segments-test--write
            (mevedel-session-artifacts-segment-path directory 2)
            "Second prompt" "Archived answer two" "fork-2" 2)
           (mevedel-view-test--with-buffers
             (with-current-buffer data-buf
               (setq-local mevedel--session session)
               (insert "Live prompt\n")
               (insert (propertize "Live answer\n" 'gptel 'response)))
             (with-current-buffer view-buf
               (setq-local mevedel--session session)
               (mevedel-view--full-rerender)
               ,@body)))
       (delete-directory directory t))))

(defun mevedel-view-segments-test--write-repeated (path)
  "Write two identical prompts with distinct response bounds to PATH."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
    (let (bounds positions)
      (dotimes (index 2)
        (insert "Same prompt\n")
        (insert (format "Answer %d\n" index)))
      (dotimes (_ 8)
        (setq bounds nil)
        (goto-char (point-min))
        (dotimes (index 2)
          (search-forward (format "Answer %d" index))
          (push (list (match-beginning 0) (match-end 0)) bounds))
        (org-entry-put (point-min) "GPTEL_BOUNDS"
                       (prin1-to-string
                        (list (cons 'response (nreverse bounds))))))
      (write-region (point-min) (point-max) path nil 'silent)
      (goto-char (point-min))
      (dotimes (_ 2)
        (search-forward "Same prompt")
        (push (match-beginning 0) positions))
      (nreverse positions))))

(defun mevedel-view-segments-test--write-directive (path)
  "Write a persisted directive turn with trusted audit bounds to PATH."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
    (insert (mevedel--format-hook-audit-record
             '(:type directive-turn-boundary :edge start
               :directive-id "d-1" :action discuss :turn 1)))
    (insert "Directive prompt\nDirective answer\n")
    (insert (mevedel--format-hook-audit-record
             '(:type directive-turn-boundary :edge end
               :directive-id "d-1" :action discuss :turn 1
               :outcome success :sequence 1)))
    (dotimes (_ 8)
      (let (audits)
        (goto-char (point-min))
        (dotimes (_ 2)
          (search-forward "<!-- mevedel-hook-audit -->")
          (let ((start (match-beginning 0)))
            (search-forward "<!-- /mevedel-hook-audit -->")
            (push (list start (point)) audits)))
        (goto-char (point-min))
        (search-forward "Directive answer")
        (org-entry-put
         (point-min) "GPTEL_BOUNDS"
         (prin1-to-string
          (list (cons 'response (list (list (match-beginning 0)
                                            (match-end 0))))
                (cons 'mevedel-hook-audit (nreverse audits)))))))
    (write-region (point-min) (point-max) path nil 'silent)))


;;
;;; State

(mevedel-deftest mevedel-view-segments-current-number ()
  ,test
  (test)
  :doc "reports only a live archived projection"
  (mevedel-view-segments-test--with-view
    (should-not (mevedel-view-segments-current-number))
    (mevedel-view-go-to-segment 2)
    (should (= 2 (mevedel-view-segments-current-number)))
    (should (eq (mevedel-view-segments-display-buffer)
                mevedel-view-segments--buffer))))

(mevedel-deftest mevedel-view-segments-initialize ()
  ,test
  (test)
  :doc "kills the archive buffer owned by a closing view"
  (let ((view (generate-new-buffer " *mevedel-segment-owner*"))
        (archive (generate-new-buffer " *mevedel-segment-archive*")))
    (unwind-protect
        (progn
          (with-current-buffer view
            (mevedel-view-segments-initialize)
            (setq mevedel-view-segments--number 1
                  mevedel-view-segments--buffer archive))
          (kill-buffer view)
          (should-not (buffer-live-p archive)))
      (when (buffer-live-p view)
        (kill-buffer view))
      (when (buffer-live-p archive)
        (kill-buffer archive)))))


;;
;;; Navigation

(mevedel-deftest mevedel-view-previous-segment ()
  ,test
  (test)
  :doc "shows exactly the adjacent archived segment as read-only"
  (mevedel-view-segments-test--with-view
    (mevedel-view-test--insert-composer-draft "live draft" 4)
    (mevedel-view-previous-segment)
    (should (eq 'assistant
                (get-text-property (point) 'mevedel-view-turn-role)))
    (should (string-prefix-p "segments @ mevedel\n" (buffer-string)))
    (should (string-search
             "Viewing archived segment 2 of 3" (buffer-string)))
    (goto-char (point-min))
    (search-forward "[Latest]")
    (should
     (eq #'mevedel-view-return-to-latest-segment
         (lookup-key
          (get-text-property (1- (point)) 'keymap)
          (kbd "RET"))))
    (should (string-search "Archived answer two" (buffer-string)))
    (should-not (string-search "Archived answer one" (buffer-string)))
    (should-not (string-search "Live answer" (buffer-string)))
    (should buffer-read-only)
    (should (invisible-p (mevedel-view--input-start))))

  :doc "a missing adjacent segment leaves the current projection unchanged"
  (let* ((directory (make-temp-file "mevedel-view-segment-gap-" t))
         (missing (mevedel-session-artifacts-segment-path directory 2))
         (session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "segments"
           :save-path (file-name-as-directory directory)
           :current-segment 3
           :prompt-index
           '((1 . ((:cum-turn 1 :preview "first prompt")))
             (2 . ((:cum-turn 2 :preview "missing prompt")))
             (3 . ((:cum-turn 3 :preview "live prompt")))))))
    (unwind-protect
        (progn
          (mevedel-view-segments-test--write
           (mevedel-session-artifacts-segment-path directory 1)
           "First prompt" "Archived answer one" "fork-1" 1)
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session)
              (insert "Live prompt\n")
              (insert (propertize "Live answer\n" 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (mevedel-view--full-rerender)
              (let ((before (buffer-string))
                    (error
                     (should-error
                      (mevedel-view-previous-segment)
                      :type 'user-error)))
                (should (string-search missing
                                       (error-message-string error)))
                (should (equal before (buffer-string)))))))
      (delete-directory directory t)))

  :doc "does not split a complete archived response at a stale saved bound"
  (let* ((directory (make-temp-file "mevedel-view-stale-bound-" t))
         (session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "segments"
           :save-path (file-name-as-directory directory)
           :current-segment 2
           :prompt-index
           '((1 . ((:cum-turn 1 :preview "archived prompt")))
             (2 . ((:cum-turn 2 :preview "live prompt")))))))
    (unwind-protect
        (progn
          (mevedel-view-segments-test--write
           (mevedel-session-artifacts-segment-path directory 1)
           "Archived prompt" "Complete archived answer."
           "fork-1" 1 (length "Complete"))
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session)
              (insert "Live prompt\n")
              (insert (propertize "Live answer\n" 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (mevedel-view--full-rerender)
              (mevedel-view-previous-segment)
              (should (string-search "Complete archived answer."
                                     (buffer-string)))
              (should (= 1 (how-many "^You$" (point-min)
                                     (mevedel-view--input-marker-position)))))))
      (delete-directory directory t)))

  :doc "is defined by the historical segment owner"
  (should
   (equal "mevedel-view-segments"
          (file-name-base
           (or (symbol-file 'mevedel-view-previous-segment 'defun) "")))))

(mevedel-deftest mevedel-view-next-segment ()
  ,test
  (test)
  :doc "revisits an archived segment with its point and fold state"
  (mevedel-view-segments-test--with-view
    (mevedel-view-go-to-segment 2)
    (goto-char (point-min))
    (search-forward "Assistant")
    (beginning-of-line)
    (mevedel-view--collapse-turn)
    (let ((archived-point (point)))
      (should (get-text-property (point) 'mevedel-view-collapsed))
      (mevedel-view-previous-segment)
      (mevedel-view-next-segment)
      (should (= archived-point (point)))
      (should (get-text-property (point) 'mevedel-view-collapsed)))))

(mevedel-deftest mevedel-view-go-to-segment ()
  ,test
  (test)
  :doc "pinned header uses only the currently displayed archived segment"
  (save-window-excursion
    (mevedel-view-segments-test--with-view
      (set-window-buffer (selected-window) view-buf)
      (mevedel-view-go-to-segment 1)
      (goto-char (point-min))
      (search-forward "Archived answer one")
      (set-window-start nil (line-beginning-position) t)
      (should (string-search "First prompt" (mevedel-view--sticky-prompt-line)))
      (mevedel-view-go-to-segment 2)
      (goto-char (point-min))
      (search-forward "Archived answer two")
      (set-window-start nil (line-beginning-position) t)
      (should (string-search "Second prompt" (mevedel-view--sticky-prompt-line)))
      (should-not (string-search "First prompt" (mevedel-view--sticky-prompt-line)))))

  :doc "direct selection bypasses a missing intervening segment"
  (let* ((directory (make-temp-file "mevedel-view-segment-picker-" t))
         (session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "segments"
           :save-path (file-name-as-directory directory)
           :current-segment 3
           :prompt-index
           '((1 . ((:cum-turn 1 :preview "first prompt")))
             (2 . ((:cum-turn 2 :preview "missing prompt")))
             (3 . ((:cum-turn 3 :preview "live prompt")))))))
    (unwind-protect
        (progn
          (mevedel-view-segments-test--write
           (mevedel-session-artifacts-segment-path directory 1)
           "First prompt" "Archived answer one" "fork-1" 1)
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session)
              (insert "Live prompt\n")
              (insert (propertize "Live answer\n" 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (mevedel-view--full-rerender)
              (mevedel-view-go-to-segment 1)
              (should (string-search "Archived answer one"
                                     (buffer-string))))))
      (delete-directory directory t))))

(mevedel-deftest mevedel-view-segments-jump-to-prompt ()
  ,test
  (test)
  :doc "lands on the exact archived prompt despite identical previews"
  (save-window-excursion
    (mevedel-view-segments-test--with-view
      (let* ((path (mevedel-session-artifacts-segment-path directory 1))
             (positions (mevedel-view-segments-test--write-repeated path))
             (window (selected-window)))
        (set-window-buffer window view-buf)
        (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
        (mevedel-view-segments-jump-to-prompt 1 (cadr positions) window)
        (should (= 1 (mevedel-view-segments-current-number)))
        (should (eq window (selected-window)))
        (should (eq 'user (get-text-property (point) 'mevedel-view-turn-role)))
        (should (<= (mevedel-view-disclosure-source-start
                     (get-text-property (point) 'mevedel-view-source))
                    (cadr positions)))
        (should (< (car positions)
                   (plist-get (plist-get (get-text-property
                                          (point) 'mevedel-view-turn-context)
                                         :turn)
                              :start)))
        (should (string-search "Answer 1" (buffer-string)))
        (should buffer-read-only)
        (mevedel-view-return-to-latest-segment)
        (should (equal "> draft\nsecond line" (mevedel-view--input-text))))))

  :doc "rejects non-prompt positions before changing the projection"
  (mevedel-view-segments-test--with-view
    (let ((before (buffer-string)))
      (should-error (mevedel-view-segments-jump-to-prompt 1 1)
                    :type 'user-error)
      (should-not (mevedel-view-segments-current-number))
      (should (equal before (buffer-string)))))

  :doc "a missing archive does not replace the live view"
  (mevedel-view-segments-test--with-view
    (delete-file (mevedel-session-artifacts-segment-path directory 1))
    (let ((before (buffer-string)))
      (should-error (mevedel-view-segments-jump-to-prompt 1 44)
                    :type 'user-error)
      (should-not (mevedel-view-segments-current-number))
      (should (equal before (buffer-string)))))

  :doc "materializes a pending target and ignores obsolete batch callbacks"
  (mevedel-view-segments-test--with-view
    (let ((pos (car (mevedel-view-segments-test--write-repeated
                     (mevedel-session-artifacts-segment-path directory 1)))))
      (mevedel-view-go-to-segment 1)
      (mevedel-view-render-batched-full)
      (should mevedel-view-render--batch)
      (mevedel-view-segments-jump-to-prompt 1 pos)
      (should-not mevedel-view-render--batch)
      (should (eq 'user (get-text-property (point) 'mevedel-view-turn-role)))
      (should (<= (mevedel-view-disclosure-source-start
                   (get-text-property (point) 'mevedel-view-source))
                  pos))
      (mevedel-view-return-to-latest-segment)
      (should-not mevedel-view-render--batch)
      (should-not (mevedel-view-segments-current-number))))

  :doc "lands on a directive indexed at its boundary before the user body"
  (mevedel-view-segments-test--with-view
    (mevedel-view-segments-test--write-directive
     (mevedel-session-artifacts-segment-path directory 1))
    (let* ((archive (mevedel-session-artifacts-read-segment session 1))
           (prompt (car (mevedel-session-artifacts-collect-prompts archive))))
      (unwind-protect
          (progn
            (should (eq (plist-get prompt :kind) 'directive))
            (mevedel-view-segments-jump-to-prompt
             1 (plist-get prompt :pos))
            (should (eq 'directive
                        (get-text-property (point) 'mevedel-view-turn-role))))
        (kill-buffer archive)))))

(mevedel-deftest mevedel-view-return-to-latest-segment ()
  ,test
  (test)
  :doc "restores the exact live composer text and point"
  (mevedel-view-segments-test--with-view
    (mevedel-view-test--insert-composer-draft
     "> live draft\nsecond line" 7)
    (let ((draft (buffer-substring
                  (mevedel-view--input-start) (point-max)))
          (point-offset (- (point) (mevedel-view--input-start))))
      (mevedel-view-previous-segment)
      (mevedel-view-return-to-latest-segment)
      (should (string-search "Live answer" (buffer-string)))
      (should-not (string-search "Viewing archived segment"
                                 (buffer-string)))
      (should-not buffer-read-only)
      (should-not (invisible-p (mevedel-view--input-start)))
      (should (equal draft
                     (buffer-substring
                      (mevedel-view--input-start) (point-max))))
      (should (= point-offset
                 (- (point) (mevedel-view--input-start)))))))

(mevedel-deftest mevedel-view-segments/reentry ()
  ,test
  (test)
  :doc "archive switching during fontification replaces, rather than mixes, sources"
  (mevedel-view-segments-test--with-view
    (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
    (let ((original (symbol-function 'mevedel-view--fontify-response))
          entered)
      ;; Avoid the response cache so the test visits the interruption seam.
      (clrhash mevedel-view--response-fontify-cache)
      (cl-letf (((symbol-function 'mevedel-view--fontify-response)
                 (lambda (&rest args)
                   (prog1 (apply original args)
                     (unless entered
                       (setq entered t)
                       (mevedel-view-go-to-segment 2))))))
        (mevedel-view--full-rerender))
      (should entered))
    (should (= 2 (mevedel-view-segments-current-number)))
    (should (string-search "Archived answer two" (buffer-string)))
    (should-not (string-search "Live answer" (buffer-string)))
    (should-not mevedel-view-render--owner)
    (mevedel-view-return-to-latest-segment)
    (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
    (should (= (point) (+ 4 (mevedel-view--input-start)))))

  :doc "return to live during archive rendering discards queued archive work"
  (mevedel-view-segments-test--with-view
    (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
    (mevedel-view-go-to-segment 2)
    (let ((archive mevedel-view-segments--buffer)
          (original (symbol-function 'mevedel-view--fontify-response)) entered stale)
      (cl-letf (((symbol-function 'mevedel-view--fontify-response)
                 (lambda (&rest args)
                   (prog1 (apply original args)
                     (unless entered
                       (setq entered t)
                       (mevedel-view-render-mutate 'archive-work
                                                   (lambda () (setq stale t)))
                       (mevedel-view-return-to-latest-segment))))))
        (mevedel-view--full-rerender))
      (should entered)
      (should-not stale)
      (should-not (buffer-live-p archive)))
    (should-not (mevedel-view-segments-current-number))
    (should (eq data-buf (mevedel-view-segments-display-buffer)))
    (should (string-search "Live answer" (buffer-string)))
    (should-not (string-search "Archived answer" (buffer-string)))
    (should-not mevedel-view-render--owner)
    (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
    (should (= (point) (+ 4 (mevedel-view--input-start))))))

(provide 'test-mevedel-view-segments)
;;; test-mevedel-view-segments.el ends here
