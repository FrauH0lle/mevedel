;;; test-niwa-view-render-archived-completion.el --- Archived runtime replay -*- lexical-binding: t -*-

;;; Commentary:
;; Runtime completions belong to the live transcript even when the tool's
;; original row is displayed from an unchanged archived segment.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-render)
(require 'niwa-tools)

(niwa-deftest niwa-view-render/archived-completion-late-bound
  (:doc "An unchanged archived Bash row reflects later current completion.")
  (let ((batch ,mode))
    (niwa-view-test--with-buffers
      (niwa-tools-register)
      (let ((archive (generate-new-buffer " *archived-completion-test*"))
            (session (niwa-session--create :name "Archived completion"
                                              :current-segment 2)))
        (unwind-protect
            (progn
              (with-current-buffer data-buf (setq-local niwa--session session))
              (with-current-buffer archive
                (niwa--transcript-org-mode)
                (setq-local niwa--view-buffer view-buf niwa--session session)
                (insert "#+begin_tool\n")
                (let ((start (point)))
                  (insert "(:name \"Bash\" :args (:command \"sleep 10\"))\nOLD OUTPUT\n"
                          (niwa-tool-render-data-format
                           '(:execution-id "archived-test-exec" :state completed :status success
                             :execution-output "OLD OUTPUT") "original"))
                  (put-text-property start (point) 'gptel '(tool . "original")))
                (insert "#+end_tool\n"))
              (with-current-buffer view-buf
                (setq-local niwa--session session)
                (setq-local niwa-view-segments--number 1
                            niwa-view-segments--buffer archive)
                (niwa-view--full-rerender archive t)
                (goto-char (point-min))
                (search-forward "sleep 10")
                (niwa-view-toggle-section)
                (should (string-search "OLD OUTPUT" (buffer-string)))
                (niwa-view--full-rerender archive)
                (niwa-view-test--insert-composer-draft "> draft\nsecond line")
                (when (memq batch '(before clear refresh))
                  (niwa-view-render-batched-full)
                  (cl-loop repeat 150
                           until (eq 'finish (plist-get niwa-view-render--batch
                                                       :prepare-stage))
                           do (niwa-view-render--batch-step
                               view-buf niwa-view-render--batch))
                  (should (eq 'finish (plist-get niwa-view-render--batch
                                                :prepare-stage)))
                  (should (> (hash-table-count
                              (plist-get niwa-view-render--batch :tool-cache)) 0)))
                (let ((tick (with-current-buffer archive (buffer-modified-tick))))
                  (with-current-buffer data-buf
                    (insert (niwa--format-hook-audit-record
                             '(:type execution-completion :tool-use-id "original"
                               :render-data (:execution-id "archived-test-exec" :status error
                                             :outcome failure :exit-code 2
                                             :execution-output "FINAL OUTPUT")))))
                  (should (equal "FINAL OUTPUT"
                                 (plist-get
                                  (niwa-view--execution-terminal-render-data
                                   archive "original" "archived-test-exec")
                                  :execution-output)))
                  (when (eq batch 'clear)
                    (clrhash (plist-get niwa-view-render--batch :tool-cache)))
                  (when (eq batch 'refresh)
                    (let ((job niwa-view-render--batch))
                      (should (niwa-view--refresh-tool-row archive "original"))
                      (should-not niwa-view-render--batch)
                      ;; A retired callback cannot publish its stale entry.
                      (niwa-view-render--batch-step view-buf job)))
                  (if batch
                      (progn
                        (when (eq batch 'after)
                          (niwa-view-render-batched-full))
                        (cl-loop repeat 150 while niwa-view-render--batch do
                                 (niwa-view-render--batch-step
                                  view-buf niwa-view-render--batch))
                        (should-not niwa-view-render--batch))
                    (niwa-view--full-rerender archive))
                  (should (= tick (with-current-buffer archive (buffer-modified-tick))))
                  (should (string-search "FINAL OUTPUT" (buffer-string)))
                  (should-not (string-search "OLD OUTPUT" (buffer-string)))
                  (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t))))))
          (when (buffer-live-p archive) (kill-buffer archive))))))
  (mode)
  :doc "A synchronous projection sees a live completion."
  nil
  :doc "A batch started after completion sees the final output."
  'after
  :doc "A prepared historical batch sees a completion appended only to live data."
  'before
  :doc "Clearing normalized entries avoids stale historical publication."
  'clear
  :doc "A tool-row writer retires the batch before publishing fresh output."
  'refresh)

(provide 'test-niwa-view-render-archived-completion)
;;; test-niwa-view-render-archived-completion.el ends here
