;;; test-niwa-view-segments.el -- Historical segment view tests -*- lexical-binding: t -*-

;;; Commentary:

;; Historical session segment projection and navigation coverage.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-session-artifacts)
(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name)) "niwa-session-test-support"))
(require 'niwa-structs)
(require 'niwa-tool-render-data)
(require 'niwa-transcript-audit)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-render)
(require 'niwa-view-segments)

(defun niwa-view-segments-test--draft-offset ()
  "Return the caret offset from the draft start, read in the composer buffer."
  (let ((view (current-buffer)))
    (with-current-buffer (niwa-view-composer-buffer view)
      (- (point) (niwa-view-composer-draft-start view)))))

(defun niwa-view-segments-test--write
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
      (insert (niwa-tool-render-data-format
               '(:kind request-summary)))
      (insert
       (niwa--format-hook-audit-record
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
        (search-forward "<!-- niwa-render-data -->")
        (setq render-start (match-beginning 0))
        (search-forward "<!-- /niwa-render-data -->")
        (setq render-end (point))
        (search-forward "<!-- niwa-hook-audit -->")
        (setq audit-start (match-beginning 0))
        (search-forward "<!-- /niwa-hook-audit -->")
        (setq audit-end (point))
        (let ((bounds
               (append
                (list
                 (list 'response
                       (list response-start
                             (+ response-start
                                (or response-bound-length
                                    (length response)))))
                 (list 'niwa-render-data (list render-start render-end))
                 (list 'niwa-hook-audit (list audit-start audit-end)))
                (when response-bound-length
                  (list
                   (list 'ignore
                         (list (+ response-start response-bound-length 2)
                               (+ response-start (length response)))))))))
          (org-entry-put (point-min) "GPTEL_BOUNDS"
                         (prin1-to-string bounds)))))
    (write-region (point-min) (point-max) path nil 'silent)))

(defun niwa-view-segments-test--publish (session &rest paths)
  "Publish explicit archive PATHS for read-only projection in SESSION."
  (niwa-session-publication-publish
   session (append (mapcar (lambda (path)
                             (list :path path :content
                                   (niwa-session-artifacts-read-file-raw path))) paths)
                   '((:path "session.meta.el" :content "(:fixture archive-only)" :commit-marker t))) t))

(defmacro niwa-view-segments-test--with-view (&rest body)
  "Run BODY in a rendered three-segment view with two archived segments."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "niwa-view-segments-" t))
          (session
           (test-niwa-session-persistence--leased-session directory
            :name "segments"
            :save-path (file-name-as-directory directory)
            :current-segment 3
            :prompt-index
            '((1 . ((:cum-turn 1 :preview "first prompt")))
              (2 . ((:cum-turn 2 :preview "second prompt")))
              (3 . ((:cum-turn 3 :preview "live prompt")))))))
     (unwind-protect
         (progn
           (setf (niwa-workspace-name (niwa-session-workspace session)) "niwa")
           (niwa-view-segments-test--write
            (niwa-session-artifacts-segment-path directory 1)
            "First prompt" "Archived answer one" "fork-1" 1)
           (niwa-view-segments-test--write
            (niwa-session-artifacts-segment-path directory 2)
            "Second prompt" "Archived answer two" "fork-2" 2)
           (niwa-view-segments-test--publish
            session (niwa-session-artifacts-segment-path directory 1)
            (niwa-session-artifacts-segment-path directory 2))
           (niwa-view-test--with-buffers
             (with-current-buffer data-buf
               (setq-local niwa--session session)
               (insert "Live prompt\n")
               (insert (propertize "Live answer\n" 'gptel 'response)))
             (with-current-buffer view-buf
               (setq-local niwa--session session)
               (niwa-view--full-rerender)
               ,@body)))
       (niwa-session-durability-lease-release directory session)
       (niwa-workspace-clear-registry)
       (delete-directory directory t))))

(defun niwa-view-segments-test--write-repeated (path session)
  "Write and publish two identical prompts with distinct bounds at SESSION PATH."
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
      (niwa-view-segments-test--publish session path)
      (goto-char (point-min))
      (dotimes (_ 2)
        (search-forward "Same prompt")
        (push (match-beginning 0) positions))
      (nreverse positions))))


;;
;;; State

(niwa-deftest niwa-view-segments-current-number ()
  ,test
  (test)
  :doc "reports only a live archived projection"
  (niwa-view-segments-test--with-view
    (should-not (niwa-view-segments-current-number))
    (niwa-view-go-to-segment 2)
    (should (= 2 (niwa-view-segments-current-number)))
    (should (eq (niwa-view-segments-display-buffer)
                niwa-view-segments--buffer))))

(niwa-deftest niwa-view-segments/compaction-summary ()
  ,test
  (test)
  :doc "an archived segment's own summary opens in the read-only view"
  (niwa-view-segments-test--with-view
    (with-temp-file (niwa-session-artifacts-segment-path directory 2)
      (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n"
              "#+begin_summary niwa-role=compaction-summary\n"
              "Archived handoff\n#+end_summary\n\n"
              "Second prompt\nArchived answer two\n"))
    (niwa-view-segments-test--publish session (niwa-session-artifacts-segment-path directory 2))
    (niwa-view-go-to-segment 2)
    (should (= 2 (niwa-view-segments-current-number)))
    (should (string-search "conversation compacted" (buffer-string)))
    (should-not (string-search "Archived handoff" (buffer-string)))
    (save-excursion
      (goto-char (point-min))
      (search-forward "conversation compacted")
      (niwa-view-toggle-section))
    (should (string-search "Archived handoff" (buffer-string)))
    (should (string-search "Second prompt" (buffer-string)))
    (should (= 1 (niwa-view-test--count-substring
                  "conversation compacted" (buffer-string))))
    ;; The live segment shows no compaction row.
    (niwa-view-go-to-segment 3)
    (should-not (string-search "conversation compacted" (buffer-string)))))

(niwa-deftest niwa-view-segments-initialize ()
  ,test
  (test)
  :doc "kills the archive buffer owned by a closing view"
  (let ((view (generate-new-buffer " *niwa-segment-owner*"))
        (archive (generate-new-buffer " *niwa-segment-archive*")))
    (unwind-protect
        (progn
          (with-current-buffer view
            (niwa-view-segments-initialize)
            (setq niwa-view-segments--number 1
                  niwa-view-segments--buffer archive))
          (kill-buffer view)
          (should-not (buffer-live-p archive)))
      (when (buffer-live-p view)
        (kill-buffer view))
      (when (buffer-live-p archive)
        (kill-buffer archive)))))


;;
;;; Navigation

(niwa-deftest niwa-view-previous-segment ()
  ,test
  (test)
  :doc "shows exactly the adjacent archived segment as read-only"
  (niwa-view-segments-test--with-view
    (niwa-view-test--insert-composer-draft "live draft" 4)
    (niwa-view-previous-segment)
    (should (eq 'assistant
                (get-text-property (point) 'niwa-view-turn-role)))
    (should (string-prefix-p "segments @ niwa\n" (buffer-string)))
    (should (string-search
             "Viewing archived segment 2 of 3" (buffer-string)))
    (goto-char (point-min))
    (search-forward "[Latest]")
    (should
     (eq #'niwa-view-return-to-latest-segment
         (lookup-key
          (get-text-property (1- (point)) 'keymap)
          (kbd "RET"))))
    (should (string-search "Archived answer two" (buffer-string)))
    (should-not (string-search "Archived answer one" (buffer-string)))
    (should-not (string-search "Live answer" (buffer-string)))
    (should buffer-read-only)
    (should (buffer-local-value 'buffer-read-only (niwa-view-composer-buffer))))

  :doc "a missing adjacent segment leaves the current projection unchanged"
  (let* ((directory (make-temp-file "niwa-view-segment-gap-" t))
         (missing (niwa-session-artifacts-segment-path directory 2))
         (session
          (test-niwa-session-persistence--leased-session directory
           :name "segments"
           :save-path (file-name-as-directory directory)
           :current-segment 3
           :prompt-index
           '((1 . ((:cum-turn 1 :preview "first prompt")))
             (2 . ((:cum-turn 2 :preview "missing prompt")))
             (3 . ((:cum-turn 3 :preview "live prompt")))))))
    (unwind-protect
        (progn
          (niwa-view-segments-test--write
           (niwa-session-artifacts-segment-path directory 1)
           "First prompt" "Archived answer one" "fork-1" 1)
          (niwa-view-segments-test--publish session (niwa-session-artifacts-segment-path directory 1))
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--session session)
              (insert "Live prompt\n")
              (insert (propertize "Live answer\n" 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local niwa--session session)
              (niwa-view--full-rerender)
              (let ((before (buffer-string))
                    (error
                     (should-error
                      (niwa-view-previous-segment)
                      :type 'user-error)))
                (should (string-search missing
                                       (error-message-string error)))
                (should (equal before (buffer-string)))))))
      (niwa-session-durability-lease-release directory session)
      (niwa-workspace-clear-registry)
      (delete-directory directory t)))

  :doc "does not split a complete archived response at a stale saved bound"
  (let* ((directory (make-temp-file "niwa-view-stale-bound-" t))
         (session
          (test-niwa-session-persistence--leased-session directory
           :name "segments"
           :save-path (file-name-as-directory directory)
           :current-segment 2
           :prompt-index
           '((1 . ((:cum-turn 1 :preview "archived prompt")))
             (2 . ((:cum-turn 2 :preview "live prompt")))))))
    (unwind-protect
        (progn
          (niwa-view-segments-test--write
           (niwa-session-artifacts-segment-path directory 1)
           "Archived prompt" "Complete archived answer."
           "fork-1" 1 (length "Complete"))
          (niwa-view-segments-test--publish session (niwa-session-artifacts-segment-path directory 1))
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--session session)
              (insert "Live prompt\n")
              (insert (propertize "Live answer\n" 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local niwa--session session)
              (niwa-view--full-rerender)
              (niwa-view-previous-segment)
              (should (string-search "Complete archived answer."
                                     (buffer-string)))
              (should (= 1 (how-many "^You$" (point-min)
                                     (niwa-view-transcript-tail)))))))
      (niwa-session-durability-lease-release directory session)
      (niwa-workspace-clear-registry)
      (delete-directory directory t)))

  :doc "is defined by the historical segment owner"
  (should
   (equal "niwa-view-segments"
          (file-name-base
           (or (symbol-file 'niwa-view-previous-segment 'defun) "")))))

(niwa-deftest niwa-view-next-segment ()
  ,test
  (test)
  :doc "revisits an archived segment with its point and fold state"
  (niwa-view-segments-test--with-view
    (niwa-view-go-to-segment 2)
    (goto-char (point-min))
    (search-forward "Assistant")
    (beginning-of-line)
    (niwa-view--collapse-turn)
    (let ((archived-point (point)))
      (should (get-text-property (point) 'niwa-view-collapsed))
      (niwa-view-previous-segment)
      (niwa-view-next-segment)
      (should (= archived-point (point)))
      (should (get-text-property (point) 'niwa-view-collapsed)))))

(niwa-deftest niwa-view-go-to-segment ()
  ,test
  (test)
  :doc "restored archived thinking opens by default and retains explicit fold choices"
  (niwa-view-segments-test--with-view
    (with-temp-file (niwa-session-artifacts-segment-path directory 2)
      (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n"
              "Second prompt\n#+begin_reasoning\nArchived thought\n#+end_reasoning\n"))
    (niwa-view-segments-test--publish session (niwa-session-artifacts-segment-path directory 2))
    (niwa-view-test--insert-composer-draft "> live draft\nsecond line" 4)
    (niwa-view-go-to-segment 2)
    (should (string-search "Archived thought" (buffer-string)))
    (goto-char (point-min))
    (search-forward "Thinking...")
    (niwa-view-toggle-section)
    (should-not (string-search "Archived thought" (buffer-string)))
    (niwa-view-next-segment)
    (should (equal "> live draft\nsecond line" (niwa-view-composer-draft-text)))
    (should (= 4 (niwa-view-segments-test--draft-offset)))
    (niwa-view-previous-segment)
    (should-not (string-search "Archived thought" (buffer-string)))
    (goto-char (point-min))
    (search-forward "Thinking...")
    (niwa-view-toggle-section)
    (should (string-search "Archived thought" (buffer-string)))
    (niwa-view-next-segment)
    (niwa-view-previous-segment)
    (should (string-search "Archived thought" (buffer-string))))
  :doc "pinned header uses only the currently displayed archived segment"
  (save-window-excursion
    (niwa-view-segments-test--with-view
      (set-window-buffer (selected-window) view-buf)
      (niwa-view-go-to-segment 1)
      (goto-char (point-min))
      (search-forward "Archived answer one")
      (set-window-start nil (line-beginning-position) t)
      (should (string-search "First prompt" (niwa-view--sticky-prompt-line)))
      (niwa-view-go-to-segment 2)
      (goto-char (point-min))
      (search-forward "Archived answer two")
      (set-window-start nil (line-beginning-position) t)
      (should (string-search "Second prompt" (niwa-view--sticky-prompt-line)))
      (should-not (string-search "First prompt" (niwa-view--sticky-prompt-line)))))

  :doc "direct selection bypasses a missing intervening segment"
  (let* ((directory (make-temp-file "niwa-view-segment-picker-" t))
         (session
          (test-niwa-session-persistence--leased-session directory
           :name "segments"
           :save-path (file-name-as-directory directory)
           :current-segment 3
           :prompt-index
           '((1 . ((:cum-turn 1 :preview "first prompt")))
             (2 . ((:cum-turn 2 :preview "missing prompt")))
             (3 . ((:cum-turn 3 :preview "live prompt")))))))
    (unwind-protect
        (progn
          (niwa-view-segments-test--write
           (niwa-session-artifacts-segment-path directory 1)
           "First prompt" "Archived answer one" "fork-1" 1)
          (niwa-view-segments-test--publish session (niwa-session-artifacts-segment-path directory 1))
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--session session)
              (insert "Live prompt\n")
              (insert (propertize "Live answer\n" 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local niwa--session session)
              (niwa-view--full-rerender)
              (niwa-view-go-to-segment 1)
              (should (string-search "Archived answer one"
                                     (buffer-string))))))
      (niwa-session-durability-lease-release directory session)
      (niwa-workspace-clear-registry)
      (delete-directory directory t))))

(niwa-deftest niwa-view-segments-jump-to-prompt ()
  ,test
  (test)
  :doc "lands on the exact archived prompt despite identical previews"
  (save-window-excursion
    (niwa-view-segments-test--with-view
      (let* ((path (niwa-session-artifacts-segment-path directory 1))
             (positions (niwa-view-segments-test--write-repeated path session))
             (window (selected-window)))
        (set-window-buffer window view-buf)
        (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
        (niwa-view-segments-jump-to-prompt 1 (cadr positions) window)
        (should (= 1 (niwa-view-segments-current-number)))
        (should (eq window (selected-window)))
        (should (eq 'user (get-text-property (point) 'niwa-view-turn-role)))
        (should (<= (niwa-view-disclosure-source-start
                     (get-text-property (point) 'niwa-view-source))
                    (cadr positions)))
        (should (< (car positions)
                   (plist-get (plist-get (get-text-property
                                          (point) 'niwa-view-turn-context)
                                         :turn)
                              :start)))
        (should (string-search "Answer 1" (buffer-string)))
        (should buffer-read-only)
        (niwa-view-return-to-latest-segment)
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))))

  :doc "rejects non-prompt positions before changing the projection"
  (niwa-view-segments-test--with-view
    (let ((before (buffer-string)))
      (should-error (niwa-view-segments-jump-to-prompt 1 1)
                    :type 'user-error)
      (should-not (niwa-view-segments-current-number))
      (should (equal before (buffer-string)))))

  :doc "a missing archive does not replace the live view"
  (niwa-view-segments-test--with-view
    (niwa-session-publication-publish
     session '((:path "segment-0001.chat.org" :delete t)
               (:path "session.meta.el" :content "(:fixture archive-only)"
                :commit-marker t)) t)
    (let ((before (buffer-string)))
      (should-error (niwa-view-segments-jump-to-prompt 1 44)
                    :type 'user-error)
      (should-not (niwa-view-segments-current-number))
      (should (equal before (buffer-string)))))

  :doc "a read failure leaves the live projection and request markers untouched"
  (niwa-view-segments-test--with-view
    (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
    (let ((before (buffer-string))
          (request-start (with-current-buffer data-buf
                           (copy-marker (point-min))))
          (read (symbol-function 'niwa-session-artifacts-read-segment)))
      (unwind-protect
          (progn
            (setq niwa-view--data-turn-start request-start)
            (cl-letf (((symbol-function 'niwa-session-artifacts-read-segment)
                       (lambda (&rest _) (user-error "Archive read failed"))))
              (should-error (niwa-view-segments-jump-to-prompt 1 44)
                            :type 'user-error))
            (should-not (niwa-view-segments-current-number))
            (should (equal before (buffer-string)))
            (should (eq niwa-view--data-turn-start request-start))
            (let ((archive (funcall read session 1)))
              (unwind-protect
                  (niwa-view-segments-jump-to-prompt
                   1 (plist-get (car (niwa-session-artifacts-collect-prompts
                                      archive)) :pos))
                (kill-buffer archive)))
            (should (= 1 (niwa-view-segments-current-number)))
            (should (eq niwa-view--data-turn-start request-start))
            (should buffer-read-only)
            (niwa-view-return-to-latest-segment)
            (should (eq niwa-view--data-turn-start request-start))
            (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))
        (set-marker request-start nil))))

  :doc "materializes a pending target and ignores obsolete batch callbacks"
  (niwa-view-segments-test--with-view
    (let ((pos (car (niwa-view-segments-test--write-repeated
                     (niwa-session-artifacts-segment-path directory 1) session))))
      (niwa-view-go-to-segment 1)
      (niwa-view-render-batched-full)
      (should niwa-view-render--batch)
      (niwa-view-segments-jump-to-prompt 1 pos)
      (should-not niwa-view-render--batch)
      (should (eq 'user (get-text-property (point) 'niwa-view-turn-role)))
      (should (<= (niwa-view-disclosure-source-start
                   (get-text-property (point) 'niwa-view-source))
                  pos))
      (niwa-view-return-to-latest-segment)
      (should-not niwa-view-render--batch)
      (should-not (niwa-view-segments-current-number)))))

(niwa-deftest niwa-view-return-to-latest-segment ()
  ,test
  (test)
  :doc "restores the exact live composer text and point"
  (niwa-view-segments-test--with-view
    (niwa-view-test--insert-composer-draft
     "> live draft\nsecond line" 7)
    (let ((draft (niwa-view-composer-draft-text))
          (point-offset (niwa-view-segments-test--draft-offset)))
      (niwa-view-previous-segment)
      (niwa-view-return-to-latest-segment)
      (should (string-search "Live answer" (buffer-string)))
      (should-not (string-search "Viewing archived segment"
                                 (buffer-string)))
      (should-not buffer-read-only)
      (should-not (buffer-local-value 'buffer-read-only (niwa-view-composer-buffer)))
      (should (equal draft (niwa-view-composer-draft-text)))
      (should (= point-offset (niwa-view-segments-test--draft-offset))))))

(niwa-deftest niwa-view-segments/reentry ()
  ,test
  (test)
  :doc "archive switching during fontification replaces, rather than mixes, sources"
  (niwa-view-segments-test--with-view
    (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
    (let ((original (symbol-function 'niwa-view--fontify-response))
          entered)
      ;; Avoid the response cache so the test visits the interruption seam.
      (clrhash niwa-view--response-fontify-cache)
      (cl-letf (((symbol-function 'niwa-view--fontify-response)
                 (lambda (&rest args)
                   (prog1 (apply original args)
                     (unless entered
                       (setq entered t)
                       (niwa-view-go-to-segment 2))))))
        (niwa-view--full-rerender))
      (should entered))
    (should (= 2 (niwa-view-segments-current-number)))
    (should (string-search "Archived answer two" (buffer-string)))
    (should-not (string-search "Live answer" (buffer-string)))
    (should-not niwa-view-render--owner)
    (niwa-view-return-to-latest-segment)
    (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
    (should (= 4 (niwa-view-segments-test--draft-offset))))

  :doc "return to live during archive rendering discards queued archive work"
  (niwa-view-segments-test--with-view
    (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
    (niwa-view-go-to-segment 2)
    (let ((archive niwa-view-segments--buffer)
          (original (symbol-function 'niwa-view--fontify-response)) entered stale)
      (cl-letf (((symbol-function 'niwa-view--fontify-response)
                 (lambda (&rest args)
                   (prog1 (apply original args)
                     (unless entered
                       (setq entered t)
                       (niwa-view-render-mutate 'archive-work
                                                   (lambda () (setq stale t)))
                       (niwa-view-return-to-latest-segment))))))
        (niwa-view--full-rerender))
      (should entered)
      (should-not stale)
      (should-not (buffer-live-p archive)))
    (should-not (niwa-view-segments-current-number))
    (should (eq data-buf (niwa-view-segments-display-buffer)))
    (should (string-search "Live answer" (buffer-string)))
    (should-not (string-search "Archived answer" (buffer-string)))
    (should-not niwa-view-render--owner)
    (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
    (should (= 4 (niwa-view-segments-test--draft-offset)))))

(niwa-deftest niwa-view-segments/fileless-agent-has-no-parent-history ()
  ,test
  (test)
  :doc "a resident fileless agent never navigates into parent session archives"
  (let* ((directory (make-temp-file "niwa-fileless-agent-" t))
         (session (test-niwa-session-persistence--leased-session directory
                   :current-segment 3))
         (data (generate-new-buffer " *fileless agent data*"))
         (view (generate-new-buffer " *fileless agent view*")))
    (unwind-protect
        (progn
          (niwa-view-segments-test--write
           (niwa-session-artifacts-segment-path directory 1)
           "Parent prompt" "Parent answer" "fork-1" 1)
          (niwa-view-segments-test--publish session (niwa-session-artifacts-segment-path directory 1))
          (with-current-buffer data
            (setq-local niwa--session session)
            (org-mode)
            (insert (propertize "Agent answer\n" 'gptel 'response)))
          (niwa-view--setup view data
                               '(:agent-transcript-p t :agent-path "/root/worker"))
          (with-current-buffer view
            (niwa-view--full-rerender)
            (should (equal '((readable . t))
                           (mapcar (lambda (descriptor)
                                     (cons (plist-get descriptor :status)
                                           (plist-get descriptor :current-p)))
                                   (niwa-session-artifacts-transcript-segments
                                    session data t))))
            (should (= 1 (length (niwa-view-segments--entries session))))
            (let (notice)
              (cl-letf (((symbol-function 'message)
                         (lambda (format-string &rest args)
                           (setq notice (apply #'format format-string args)))))
                (niwa-view-previous-segment))
              (should (equal "niwa: oldest segment" notice)))
            (should-error (niwa-view-go-to-segment 2) :type 'user-error)
            (should-not (niwa-view-segments-current-number))
            (should (string-search "Agent answer" (buffer-string)))
            (should-not (string-search "Parent answer" (buffer-string)))))
      (when (buffer-live-p view) (kill-buffer view))
      (when (buffer-live-p data) (kill-buffer data))
      (niwa-session-durability-lease-release directory session)
      (niwa-workspace-clear-registry)
      (delete-directory directory t))))

(niwa-deftest niwa-view-segments-chats-line ()
  ,test
  (test)
  :doc "names earlier chats below the header, as header chrome, and only when they exist"
  (niwa-view-segments-test--with-view
    (goto-char (point-min))
    (forward-line 1)
    (should-not (get-text-property (point) 'niwa-view-chats-line))
    (should (= (point) (niwa-view--after-header-position)))
    (setf (niwa-session-chapters session)
          '((:segment 1 :title "First chat" :closed-at "t1")
            (:segment 2 :title nil :closed-at "t2")))
    (niwa-view--full-rerender)
    (goto-char (point-min))
    (forward-line 1)
    (should (looking-at-p
             "◂ 2 earlier chats in this session: second prompt · First chat\n\n"))
    (should (get-text-property (point) 'niwa-view-chats-line))
    (should (eq #'niwa-view-go-to-chat
                (lookup-key (get-text-property (point) 'keymap) (kbd "RET"))))
    ;; History starts after the line, so the line never counts as history.
    (let ((after (niwa-view--after-header-position)))
      (should-not (get-text-property after 'niwa-view-chats-line))
      (should (get-text-property (1- after) 'niwa-view-chats-line))
      ;; Point where history starts must not inherit the line's keymap.
      (should-not (eq niwa-view-segments--chats-map
                      (get-pos-property after 'keymap)))))
  :doc "protects earlier-chat chrome but highlights only its label"
  (niwa-view-segments-test--with-view
    (setf (niwa-session-chapters session)
          '((:segment 1 :title "First chat" :closed-at "t1")))
    (niwa-view-test--insert-composer-draft "> draft\nsecond line" 7)
    (let ((draft (niwa-view-composer-draft-text))
          (draft-offset (niwa-view-segments-test--draft-offset)))
      (niwa-view--full-rerender)
      (save-excursion
        (goto-char (point-min))
        (forward-line 1)
        (let ((start (point))
              (label-end (line-end-position))
              (after (niwa-view--after-header-position)))
          (should (eq 'highlight (get-text-property start 'mouse-face)))
          (should (eq niwa-view-segments--chats-map
                      (get-text-property (1- label-end) 'keymap)))
          (cl-loop for pos from start below after do
                   (goto-char pos)
                   (should-error (insert "x") :type 'text-read-only)
                   (should-error (delete-char 1) :type 'text-read-only))
          (cl-loop for pos from label-end below after do
                   (should-not (get-text-property pos 'mouse-face))
                   (should-not (get-text-property pos 'keymap)))
          (should-not (eq niwa-view-segments--chats-map
                          (get-pos-property after 'keymap)))))
      (should (= draft-offset (niwa-view-segments-test--draft-offset)))
      (should (equal draft (niwa-view-composer-draft-text)))
      (with-current-buffer (niwa-view-composer-buffer) (insert "x"))
      (should (string-search "x" (niwa-view-composer-draft-text)))))
  :doc "an archived projection shows its banner instead"
  (niwa-view-segments-test--with-view
    (setf (niwa-session-chapters session)
          '((:segment 1 :title "First chat" :closed-at "t1")))
    (niwa-view-go-to-segment 1)
    (goto-char (point-min))
    (should-not (text-property-any (point-min) (point-max)
                                   'niwa-view-chats-line t))
    (should (text-property-any (point-min) (point-max)
                               'niwa-view-historical-banner t))))

(niwa-deftest niwa-view-go-to-chat ()
  ,test
  (test)
  :doc "opens an earlier chat at its last segment and returns to the live chat"
  (niwa-view-segments-test--with-view
    (setf (niwa-session-chapters session)
          '((:segment 2 :title "First chat" :closed-at "t1")))
    (let (offered)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (setq offered (mapcar #'car choices))
                   (car offered))))
        (niwa-view-go-to-chat))
      (should (string-match-p "\\`Chat 1  First chat +S1–S2\\'" (car offered)))
      (should (string-match-p "\\`Chat 2  segments +current\\'" (cadr offered))))
    (should (= 2 (niwa-view-segments-current-number)))
    (niwa-view-go-to-chat 2)
    (should-not (niwa-view-segments-current-number))
    (should-error (niwa-view-go-to-chat 3) :type 'user-error))
  :doc "a session with one chat has nothing to choose"
  (niwa-view-segments-test--with-view
    (should-error (niwa-view-go-to-chat) :type 'user-error)))

(niwa-deftest niwa-view-segments--candidate ()
  ,test
  (test)
  :doc "names each segment's chat only for a session that clear divided"
  (let ((segment (list :number 2 :status 'readable :preview "Prompt"
                       :chat (list :number 1 :title "Planning"))))
    (should (equal "  S2  readable    Prompt"
                   (niwa-view-segments--candidate segment 3)))
    (should (equal "  S2  readable    Chat 1 · Planning  Prompt"
                   (niwa-view-segments--candidate segment 3 t)))
    (should (equal "● S2  readable    Chat 1 · Planning  Prompt"
                   (niwa-view-segments--candidate segment 2 t)))))

(niwa-deftest niwa-view-render-capture-segment-state ()
  ,test
  (test)
  :doc "a revisit returns point and window start to the same content"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (dotimes (i 4)
        (niwa-view-test--insert-data data-buf (format "Prompt %d\n" i) nil)
        (niwa-view-test--insert-data
         data-buf (format "Response %d with target text.\n" i) 'response))
      (with-current-buffer view-buf
        (set-window-buffer (selected-window) view-buf)
        (niwa-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Response 2 with")
        (set-window-start (selected-window) (line-beginning-position) t)
        (search-forward "target")
        (let ((state (niwa-view-render-capture-segment-state)))
          ;; The next projection is longer above the reader, as a formatted
          ;; table and its raw source differ.
          (niwa-view--full-rerender)
          (let ((inhibit-read-only t))
            (goto-char (point-min))
            (insert "A longer projection of earlier history.\n"))
          (goto-char (point-min))
          (niwa-view-render--restore-segment-state state 'backward)
          (should (looking-back "Response 2 with target" (line-beginning-position)))
          (should (save-excursion
                    (goto-char (window-start))
                    (looking-at "Response 2 with")))))))
  :doc "positions without an anchor fall back to their offsets"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (let ((state (niwa-view-render-capture-segment-state)))
        (should-not (plist-get state :point-anchor))
        (goto-char (niwa-view-transcript-tail))
        (niwa-view-render--restore-segment-state state 'backward)
        (should (= (point) (point-min)))))))

(provide 'test-niwa-view-segments)
;;; test-niwa-view-segments.el ends here
