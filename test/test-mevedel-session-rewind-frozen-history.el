;;; test-mevedel-session-rewind-frozen-history.el --- Rewind with migrated directive history -*- lexical-binding: t -*-

;;; Commentary:

;; Rewind a real saved session whose discarded turn is a migrated directive
;; attempt, through the public command on real files.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-chat)
(require 'mevedel-turn)
(require 'mevedel-session-rewind)
(require 'mevedel-transcript-audit)
(require 'mevedel-persistence)
(require 'mevedel-notes)
(require 'mevedel-directive-activity)

(defun mevedel-session-rewind-frozen-history-test--store (session-id)
  "Return a format-1 store whose directive attempt settled in SESSION-ID turn 2."
  `(:format 1 :ids (:id-counter 7 :used-ids (7) :retired-ids nil)
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
      :subdirectives nil :session-id ,session-id :planning-enabled nil
      :skills nil :plan nil :planning nil :discussion nil
      :attempts ((:sequence 1 :action request-changes
                  :directive-request "Explain this code"
                  :request "Explain this code" :result "Old implementation"
                  :outcome success
                  :patch "diff --git a/work.el b/work.el\n"
                  :capture incomplete :covered-files ("work.el") :gaps nil
                  :untracked-effects (("Bash" . "external effect"))
                  :captured-at "2026-08-07T00:00:00+0200"
                  :checkpoint (:session-id ,session-id :turn 2)
                  :plan nil :plan-context nil :plan-selection nil
                  :consumed-subdirectives nil))))))

;; Named for the impact display it checks; `mevedel-rewind' and
;; `mevedel-session-rewind-rewind' already name deftests elsewhere.
(mevedel-deftest mevedel-session-rewind--render-rewind-impact
  (:quiet t)
  ,test
  (test)
  :doc "Rewind before a migrated attempt warns of its untracked effect and keeps the history"
  (let* ((mevedel--instruction-states (make-hash-table :test #'equal))
         (mevedel-instruction-store--dirty nil)
         (mevedel-instruction-store--failed nil)
         (buffers (buffer-list))
         (setup (test-mevedel-session-persistence--make-tempdir-workspace))
         (workspace (car setup))
         (tempdir (cdr setup))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer "*test-frozen-history-rewind*"))
         (work (file-name-concat tempdir "work.el"))
         (store (file-name-concat tempdir ".mevedel/instructions.el"))
         impact-text)
    (save-window-excursion
      (unwind-protect
          (progn
            ;; The old store names the real session, so it is written once
            ;; the session exists and then observed like any external write.
            (make-directory (file-name-directory store) t)
            (write-region "first code\nsecond code\n" nil
                          (file-name-concat tempdir "source.el") nil 'silent)
            (write-region (prin1-to-string
                           (mevedel-session-rewind-frozen-history-test--store
                            (mevedel-session-session-id session)))
                          nil store nil 'silent)
            (mevedel-instruction-store-observe workspace)
            (write-region "before T2" nil work nil 'silent)
            (with-current-buffer buffer
              (org-mode)
              (setq-local mevedel--session session)
              (mevedel-request-begin session)
              (insert "First prompt\n")
              (insert (propertize "First reply.\n" 'gptel 'response))
              (setf (mevedel-session-turn-count session) 1)
              (mevedel-session-artifacts-save session buffer t)
              (mevedel-request-end)
              (mevedel-request-begin session)
              (puthash work "before T2"
                       (mevedel-request-file-snapshots mevedel--current-request))
              (write-region "after T2" nil work nil 'silent)
              (insert (mevedel--format-hook-audit-record
                       '(:type directive-turn-boundary :edge start
                         :directive-id "old-directive" :action request-changes
                         :turn 2)))
              (insert "Explain this code\n")
              (insert (propertize "Old implementation\n" 'gptel 'response))
              (insert (mevedel--format-hook-audit-record
                       '(:type directive-turn-boundary :edge end
                         :directive-id "old-directive" :action request-changes
                         :turn 2 :outcome success :activity-kind attempt
                         :sequence 1)))
              (setf (mevedel-session-turn-count session) 2)
              (mevedel-session-artifacts-save session buffer t)
              (mevedel-request-end)
              (let ((choice
                     (car (cl-find 2 (mevedel-session-rewind--prompt-candidates
                                      session)
                                   :key (lambda (candidate)
                                          (plist-get (cdr candidate) :cum-turn))))))
                (should choice)
                (cl-letf (((symbol-function 'completing-read)
                           (lambda (&rest _) choice))
                          ((symbol-function 'display-buffer) #'ignore)
                          ((symbol-function 'yes-or-no-p)
                           (lambda (&rest _)
                             (setq impact-text
                                   (with-current-buffer "*mevedel-rewind-impact*"
                                     (buffer-string)))
                             t))
                          ((symbol-function 'mevedel--run-session-start-hooks)
                           #'ignore))
                  (mevedel-rewind))))
            (should (string-match-p
                     "Directive old-directive via Bash (external effect)"
                     impact-text))
            (should (= 1 (mevedel-session-turn-count session)))
            (with-current-buffer buffer
              (should (string-match-p "First prompt" (buffer-string)))
              (should-not (string-match-p "Old implementation" (buffer-string))))
            (should (equal "before T2"
                           (mevedel-session-artifacts--file-text work)))
            ;; Rewind discards the session turn, never the frozen note history.
            (with-current-buffer (find-file-noselect
                                  (file-name-concat tempdir "source.el"))
              (goto-char (point-min))
              (with-current-buffer (mevedel-note-history)
                (should (string-match-p "Request changes · T2" (buffer-string)))
                (should (string-match-p "Old implementation" (buffer-string))))))
        (test-mevedel-session-persistence--release-and-kill buffer session)
        (dolist (other (buffer-list))
          (unless (memq other buffers) (kill-buffer other)))
        (setq mevedel-instruction-store--dirty nil)
        (remove-hook 'post-command-hook #'mevedel-instruction-store-flush)
        (delete-directory tempdir t)
        (mevedel-workspace-clear-registry)))))

(provide 'test-mevedel-session-rewind-frozen-history)

;;; test-mevedel-session-rewind-frozen-history.el ends here
