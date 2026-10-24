;;; test-mevedel-view-render-archived-completion.el --- Archived runtime replay -*- lexical-binding: t -*-

;;; Commentary:
;; Runtime completions belong to the live transcript even when the tool's
;; original row is displayed from an unchanged archived segment.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-render)
(require 'mevedel-tools)

(mevedel-deftest mevedel-view-render/archived-completion-late-bound
  (:doc "An unchanged archived Bash row reflects later current completion.")
  (let ((batch ,mode))
    (mevedel-view-test--with-buffers
      (mevedel-tools-register)
      (let ((archive (generate-new-buffer " *archived-completion-test*"))
            (session (mevedel-session--create :name "Archived completion"
                                              :current-segment 2)))
        (unwind-protect
            (progn
              (with-current-buffer data-buf (setq-local mevedel--session session))
              (with-current-buffer archive
                (mevedel--transcript-org-mode)
                (setq-local mevedel--view-buffer view-buf mevedel--session session)
                (insert "#+begin_tool\n")
                (let ((start (point)))
                  (insert "(:name \"Bash\" :args (:command \"sleep 10\"))\nOLD OUTPUT\n"
                          (mevedel-tool-render-data-format
                           '(:execution-id "archived-test-exec" :state completed :status success
                             :execution-output "OLD OUTPUT") "original"))
                  (put-text-property start (point) 'gptel '(tool . "original")))
                (insert "#+end_tool\n"))
              (with-current-buffer view-buf
                (setq-local mevedel--session session)
                (setq-local mevedel-view-segments--number 1
                            mevedel-view-segments--buffer archive)
                (mevedel-view--full-rerender archive t)
                (goto-char (point-min))
                (search-forward "sleep 10")
                (mevedel-view-toggle-section)
                (should (string-search "OLD OUTPUT" (buffer-string)))
                (mevedel-view--full-rerender archive)
                (goto-char (point-max))
                (insert "> draft\nsecond line")
                (when (memq batch '(before clear refresh))
                  (mevedel-view-render-batched-full)
                  (cl-loop repeat 150
                           until (eq 'finish (plist-get mevedel-view-render--batch
                                                       :prepare-stage))
                           do (mevedel-view-render--batch-step
                               view-buf mevedel-view-render--batch))
                  (should (eq 'finish (plist-get mevedel-view-render--batch
                                                :prepare-stage)))
                  (should (> (hash-table-count
                              (plist-get mevedel-view-render--batch :tool-cache)) 0)))
                (let ((tick (with-current-buffer archive (buffer-modified-tick))))
                  (with-current-buffer data-buf
                    (insert (mevedel--format-hook-audit-record
                             '(:type execution-completion :tool-use-id "original"
                               :render-data (:execution-id "archived-test-exec" :status error
                                             :outcome failure :exit-code 2
                                             :execution-output "FINAL OUTPUT")))))
                  (should (equal "FINAL OUTPUT"
                                 (plist-get
                                  (mevedel-view--execution-terminal-render-data
                                   archive "original" "archived-test-exec")
                                  :execution-output)))
                  (when (eq batch 'clear)
                    (clrhash (plist-get mevedel-view-render--batch :tool-cache)))
                  (when (eq batch 'refresh)
                    (let ((job mevedel-view-render--batch))
                      (should (mevedel-view--refresh-tool-row archive "original"))
                      (should-not mevedel-view-render--batch)
                      ;; A retired callback cannot publish its stale entry.
                      (mevedel-view-render--batch-step view-buf job)))
                  (if batch
                      (progn
                        (when (eq batch 'after)
                          (mevedel-view-render-batched-full))
                        (cl-loop repeat 150 while mevedel-view-render--batch do
                                 (mevedel-view-render--batch-step
                                  view-buf mevedel-view-render--batch))
                        (should-not mevedel-view-render--batch))
                    (mevedel-view--full-rerender archive))
                  (should (= tick (with-current-buffer archive (buffer-modified-tick))))
                  (should (string-search "FINAL OUTPUT" (buffer-string)))
                  (should-not (string-search "OLD OUTPUT" (buffer-string)))
                  (should (equal "> draft\nsecond line" (mevedel-view--input-text))))))
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

(provide 'test-mevedel-view-render-archived-completion)
;;; test-mevedel-view-render-archived-completion.el ends here
