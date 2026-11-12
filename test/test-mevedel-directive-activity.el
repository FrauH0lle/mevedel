;;; test-mevedel-directive-activity.el -- Directive history inspector tests -*- lexical-binding: t -*-

;;; Commentary:

;; Open migrated directive history through a note's history command, from a
;; real format-1 store and source file.

;;; Code:

(require 'mevedel)
(require 'mevedel-notes)
(require 'mevedel-persistence)
(require 'mevedel-directive-activity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-directive-activity-test--store ()
  "Return a format-1 store whose directive has settled activity."
  '(:format 1 :ids (:id-counter 7 :used-ids (7) :retired-ids nil)
    :rewinds nil
    :files (("source.el" :original-content "first code\nsecond code\n"
             :instructions
             ((:overlay-start 1 :overlay-end 11
               :anchor (:uuid "old-directive" :text "first code")
               :properties (mevedel-instruction t mevedel-id 7
                            mevedel-uuid "old-directive"
                            mevedel-instruction-type directive)))))
    :directives
    ((:id "old-directive" :request "Explain this code"
      :anchor (:state attached :file "source.el" :start 1 :end 11
               :evidence (:uuid "old-directive" :text "first code"))
      :subdirectives nil :session-id "main" :planning-enabled t
      :skills nil :plan nil
      :planning ((:sequence 3 :action request-changes
                  :directive-request "Explain this code"
                  :implementation-prompt "Plan prompt"
                  :request "Plan request" :result "Plan result"
                  :outcome success :checkpoint (:session-id "main" :turn 3)))
      :attempts ((:sequence 2 :action request-changes
                  :directive-request "Explain this code"
                  :request "Implement request" :result "Implemented"
                  :outcome success
                  :patch "diff --git a/source.el b/source.el\n"
                  :capture incomplete :covered-files ("source.el")
                  :gaps (("other.el" . "capture failed"))
                  :untracked-effects (("Bash" . "untracked command effects"))
                  :captured-at "2026-08-07T00:00:00+0200"
                  :checkpoint (:session-id "main" :turn 2)
                  :plan nil :plan-context nil :plan-selection nil
                  :consumed-subdirectives nil))
      :discussion ((:sequence 1 :directive-request "Explain this code"
                    :message "Question" :request "Question prompt"
                    :result "Answer" :outcome success :attempt-index nil
                    :checkpoint (:session-id "main" :turn 4)))))))

(defmacro mevedel-directive-activity-test--history (&rest body)
  "Migrate a real store, open its note's history, then run BODY.
BODY sees the visited SOURCE buffer and the HISTORY inspector buffer."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "directive-history-" t))
          (file (file-name-concat directory "source.el"))
          (store (file-name-concat directory ".mevedel/instructions.el"))
          (mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel-workspace--registry (make-hash-table :test #'equal))
          (mevedel-instruction-store--dirty nil)
          (mevedel-instruction-store--failed nil)
          (buffers (buffer-list))
          source history)
     (save-window-excursion
       (unwind-protect
           (progn
             (make-directory (file-name-directory store) t)
             (write-region "first code\nsecond code\n" nil file nil 'silent)
             (write-region (prin1-to-string
                            (mevedel-directive-activity-test--store))
                           nil store nil 'silent)
             (mevedel-workspace-get-or-create 'file directory directory "history")
             (setq source (find-file-noselect file))
             (with-current-buffer source
               (goto-char (point-min))
               (setq history (mevedel-note-history)))
             ,@body)
         ;; History, its transcript and the patch viewer are all new buffers.
         (dolist (buffer (buffer-list))
           (unless (memq buffer buffers) (kill-buffer buffer)))
         (setq mevedel-instruction-store--dirty nil)
         (remove-hook 'post-command-hook #'mevedel-instruction-store-flush)
         (delete-directory directory t)))))

(mevedel-deftest mevedel-open-directive-activity
  (:quiet t)
  ,test
  (test)
  :doc "inspects a migrated note's stored history plist and its patch"
  (mevedel-directive-activity-test--history
    (let* ((note (cl-find-if
                  (lambda (overlay)
                    (overlay-get overlay 'mevedel-note-legacy-history))
                  (with-current-buffer source
                    (overlays-in (point-min) (point-max)))))
           (plist (overlay-get note 'mevedel-note-legacy-history))
           (workspace (mevedel--instruction-buffer-workspace source))
           (shown (with-current-buffer history
                    (should (derived-mode-p 'mevedel-directive-activity-mode))
                    (should buffer-read-only)
                    (should (string-match-p "Request changes · T2"
                                            (buffer-string)))
                    (goto-char (point-min))
                    (search-forward "Request changes · T2")
                    (mevedel-directive-activity-view-patch)
                    (buffer-substring-no-properties (point-min) (point-max)))))
      (should (cl-find-if
               (lambda (buffer)
                 (and (string-prefix-p "*mevedel:patch@" (buffer-name buffer))
                      (with-current-buffer buffer
                        (string-match-p "diff --git a/source\\.el"
                                        (buffer-string)))))
               (buffer-list)))
      ;; The persisted plist alone opens the same history, with no live record.
      (should (equal "old-directive" (plist-get plist :id)))
      (with-current-buffer (mevedel-open-directive-activity plist workspace)
        (should buffer-read-only)
        (should (equal shown (buffer-substring-no-properties
                              (point-min) (point-max))))))))

(mevedel-deftest mevedel-note-history
  (:quiet t)
  ,test
  (test)
  :doc "opens migrated activity read-only, outside the session view"
  (mevedel-directive-activity-test--history
    (with-current-buffer history
      (should (derived-mode-p 'mevedel-directive-activity-mode))
      (should-not (derived-mode-p 'mevedel-view-mode))
      (should buffer-read-only)
      (should (string-match-p "Explain this code" (buffer-string)))))
  :doc "renders every activity kind and the attempt's capture details"
  (mevedel-directive-activity-test--history
    (with-current-buffer history
      (let ((text (buffer-string)))
        (should (string-match-p "Request changes · T2" text))
        (should (string-match-p "Discuss · T4" text))
        (should (string-match-p "Plan · T3" text))
        (should (string-match-p "Implemented" text))
        (should (string-match-p "Answer" text))
        (should (string-match-p "Capture: Incomplete" text))
        (should (string-match-p "Captured at: 2026-08-07" text))
        (should (string-match-p "Checkpoint: main turn 2" text))
        (should (string-match-p "Covered files: .*source\\.el" text))
        (should (string-match-p "File gaps: .*other\\.el (capture failed)"
                                text))
        (should (string-match-p "Untracked effects: Bash" text))))))

(mevedel-deftest mevedel-directive-activity-refresh
  (:quiet t)
  ,test
  (test)
  :doc "re-renders frozen history in place without duplicating rows"
  (mevedel-directive-activity-test--history
    (with-current-buffer history
      (mevedel-directive-activity-refresh)
      (mevedel-directive-activity-refresh)
      (should buffer-read-only)
      (should (= 1 (how-many "Request changes · T2" (point-min) (point-max))))
      (should (= 1 (how-many "Discuss · T4" (point-min) (point-max)))))))

(mevedel-deftest mevedel-directive-activity-view-patch
  (:quiet t)
  ,test
  (test)
  :doc "opens the attempt row's patch and refuses a discussion row"
  (mevedel-directive-activity-test--history
    (with-current-buffer history
      (goto-char (point-min))
      (search-forward "Discuss · T4")
      (should-error (mevedel-directive-activity-view-patch) :type 'user-error)
      (goto-char (point-min))
      (search-forward "Request changes · T2")
      (mevedel-directive-activity-view-patch))
    (should (cl-find-if
             (lambda (buffer)
               (and (string-prefix-p "*mevedel:patch@" (buffer-name buffer))
                    (with-current-buffer buffer
                      (string-match-p "diff --git a/source\\.el"
                                      (buffer-string)))))
             (buffer-list)))))

(mevedel-deftest mevedel-directive-activity-goto-source
  (:quiet t)
  ,test
  (test)
  :doc "visits the migrated note's location in the real source file"
  (mevedel-directive-activity-test--history
    (with-current-buffer source (goto-char (point-max)))
    (with-current-buffer history (mevedel-directive-activity-goto-source))
    (should (eq source (window-buffer (selected-window))))
    (should (= 1 (window-point (selected-window))))))

(provide 'test-mevedel-directive-activity)

;;; test-mevedel-directive-activity.el ends here
