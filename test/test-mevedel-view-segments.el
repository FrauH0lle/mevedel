;;; test-mevedel-view-segments.el -- Historical segment view tests -*- lexical-binding: t -*-

;;; Commentary:

;; Historical session segment projection and navigation coverage.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-chat)
(require 'mevedel-pipeline)
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

(provide 'test-mevedel-view-segments)
;;; test-mevedel-view-segments.el ends here
